package com.chiu.megalith.security.component.provider;

import com.chiu.megalith.manage.service.UserService;
import com.chiu.megalith.security.user.LoginUser;
import com.chiu.megalith.base.lang.Const;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2023-01-14 9:02
 */
@Component
public final class PasswordAuthenticationProvider extends ProviderSupport {

    private final PasswordEncoder passwordEncoder;

    private final StringRedisTemplate redisTemplate;

    private final UserService userService;

    @Value("${blog.password-error-intervalTime}")
    private long intervalTime;

    @Value("${blog.email-try-count}")
    private int maxTryNum;

    public PasswordAuthenticationProvider(PasswordEncoder passwordEncoder,
                                          StringRedisTemplate redisTemplate,
                                          UserService userService,
                                          UserDetailsService userDetailsService) {
        super(Const.GRANT_TYPE_PASSWORD.getInfo(), userDetailsService);
        this.passwordEncoder = passwordEncoder;
        this.redisTemplate = redisTemplate;
        this.userService = userService;
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

    private final RedisScript<Void> lua = RedisScript.of(
            "redis.call('ltrim', KEYS[1], ARGV[1], ARGV[2]);" +
            "redis.call('rpush', KEYS[1], ARGV[3]);" +
            "redis.call('expire', KEYS[1], ARGV[4])");

    private void passwordNotMatchProcess(String username) {
        String prefix = Const.PASSWORD_KEY.getInfo() + username;
        List<String> loginFailureTimeStampRecords = redisTemplate.opsForList().range(prefix, 0, -1);
        int len = loginFailureTimeStampRecords.size();
        int r = - 1;
        int l = 0;

        long currentTimeMillis = System.currentTimeMillis();
        for (int i = loginFailureTimeStampRecords.size() - 1; i >= 0; i--) {
            long timestamp = Long.parseLong(loginFailureTimeStampRecords.get(i));
            if (currentTimeMillis - timestamp >= intervalTime) {
                l++;
            } else {
                break;
            }
        }

        if (len - l + 1 >= maxTryNum) {
            userService.changeUserStatusByUsername(username, 1);
        }

        redisTemplate.execute(lua, Collections.singletonList(prefix),
                String.valueOf(l), String.valueOf(r), String.valueOf(System.currentTimeMillis()), String.valueOf(intervalTime / 1000));
    }
}
