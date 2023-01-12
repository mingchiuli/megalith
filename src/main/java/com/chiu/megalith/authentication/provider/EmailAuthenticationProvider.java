package com.chiu.megalith.authentication.provider;

import com.chiu.megalith.common.lang.Const;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.Map;

/**
 * @author mingchiuli
 * @create 2022-12-30 10:57 am
 */
@RequiredArgsConstructor
public class EmailAuthenticationProvider extends DaoAuthenticationProvider {

    private final StringRedisTemplate redisTemplate;

    @Value("${blog.email-try-count}")
    private Integer maxTryNum;

    @SneakyThrows
    @Override
    protected void additionalAuthenticationChecks(UserDetails userDetails, UsernamePasswordAuthenticationToken authentication) {
        //username is login email
        String prefix = Const.EMAIL_KEY.getMsg() + userDetails.getUsername();
        Map<Object, Object> entries = redisTemplate.opsForHash().entries(prefix);

        if (entries.size() == 2) {
            String code = (String) entries.get("code");
            String tryCount = (String) entries.get("tryCount");

            if (Integer.parseInt(tryCount) >= maxTryNum) {
                redisTemplate.delete(prefix);
                throw new BadCredentialsException("code reach max try number");
            }

            if (!code.equals(authentication.getCredentials())) {
                redisTemplate.opsForHash().increment(prefix, "tryCount", 1);
                throw new BadCredentialsException("code mismatch");
            }

            redisTemplate.delete(prefix);
        } else {
            throw new BadCredentialsException("code non exist");
        }

    }
}
