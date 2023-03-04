package com.chiu.megalith.security.component.provider;

import com.chiu.megalith.manage.service.UserService;
import com.chiu.megalith.security.user.LoginUser;
import com.chiu.megalith.common.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.SessionCallback;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.lang.NonNull;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

/**
 * @author mingchiuli
 * @create 2023-01-14 9:02
 */
@RequiredArgsConstructor
public class PasswordAuthenticationProvider extends ProviderSupport {

    private final PasswordEncoder passwordEncoder;

    private final StringRedisTemplate redisTemplate;

    private final UserService userService;

    private static final int intervalTime = 15 * 60 * 1000;

    @Value("${blog.email-try-count}")
    private int maxTryNum;

    @Override
    public boolean supports(String grantType) {
        return Const.GRANT_TYPE_PASSWORD.getInfo().equals(grantType);
    }

    @Override
    public void authProcess(LoginUser user,
                            UsernamePasswordAuthenticationToken authentication) {

        Optional.ofNullable(authentication.getCredentials()).ifPresentOrElse(credentials -> {
            String presentedPassword = credentials.toString();
            if (!passwordEncoder.matches(presentedPassword, user.getPassword())) {
                String username = user.getUsername();
                passwordNotMatchProcess(username);
                throw new BadCredentialsException("Failed to authenticate since password does not match stored value");
            }
        }, () -> {
            throw new BadCredentialsException("Failed to authenticate since no credentials provided");
        });
    }

    private void passwordNotMatchProcess(String username) {
        String prefix = Const.PASSWORD_KEY.getInfo() + username;
        List<String> loginFailureTimeStampRecords = redisTemplate.opsForList().range(prefix, 0, -1);
        int len = loginFailureTimeStampRecords.size();
        int r = -len - 1;
        for (String timeStamp : loginFailureTimeStampRecords) {
            long currentTimeMillis = System.currentTimeMillis();
            long ts = Long.parseLong(timeStamp);
            if (currentTimeMillis - ts < intervalTime) {
                r++;
            } else {
                break;
            }
        }

        if (len + r + 1 >= maxTryNum - 1) {
            userService.changeUserStatusByUsername(username, 1);
        }

        int rEnd = r;

        redisTemplate.execute(new SessionCallback<>() {
            @Override
            @SuppressWarnings("unchecked")
            public List<Object> execute(@NonNull RedisOperations operations) throws DataAccessException {
                operations.multi();
                operations.opsForList().trim(prefix, 0, rEnd);
                operations.opsForList().leftPush(prefix, String.valueOf(System.currentTimeMillis()));
                operations.expire(prefix, intervalTime, TimeUnit.MILLISECONDS);
                return operations.exec();
            }
        });
    }
}
