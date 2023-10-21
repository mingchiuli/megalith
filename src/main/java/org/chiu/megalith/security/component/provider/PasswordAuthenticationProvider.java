package org.chiu.megalith.security.component.provider;

import org.chiu.megalith.infra.utils.LuaScriptUtils;
import org.chiu.megalith.manage.service.UserService;
import org.chiu.megalith.security.user.LoginUser;
import org.chiu.megalith.infra.lang.Const;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.chiu.megalith.infra.lang.ExceptionMessage.PASSWORD_MISMATCH;
import static org.chiu.megalith.infra.lang.ExceptionMessage.PASSWORD_MISS;

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
    public void authProcess(LoginUser user, UsernamePasswordAuthenticationToken authentication) {

        Optional.ofNullable(authentication.getCredentials()).ifPresentOrElse(credentials -> {
            String presentedPassword = credentials.toString();
            if (!passwordEncoder.matches(presentedPassword, user.getPassword())) {
                String username = user.getUsername();
                passwordNotMatchProcess(username);
                throw new BadCredentialsException(PASSWORD_MISMATCH.getMsg());
            }
        }, () -> {
            throw new BadCredentialsException(PASSWORD_MISS.getMsg());
        });
    }

    private void passwordNotMatchProcess(String username) {
        String prefix = Const.PASSWORD_KEY.getInfo() + username;
        List<String> loginFailureTimeStampRecords = Optional.ofNullable(redisTemplate.opsForList().range(prefix, 0, -1)).orElseGet(ArrayList::new);
        int len = loginFailureTimeStampRecords.size();
        int l = 0;

        long currentTimeMillis = System.currentTimeMillis();

        for (String timestamp : loginFailureTimeStampRecords) {
            if (currentTimeMillis - Long.parseLong(timestamp) >= intervalTime) {
                l++;
            } else {
                break;
            }
        }

        if (len - l + 1 >= maxTryNum) {
            userService.changeUserStatusByUsername(username, 1);
        }

        redisTemplate.execute(LuaScriptUtils.passwordLua, Collections.singletonList(prefix),
                String.valueOf(l), "-1", String.valueOf(System.currentTimeMillis()), String.valueOf(intervalTime / 1000));
    }
}
