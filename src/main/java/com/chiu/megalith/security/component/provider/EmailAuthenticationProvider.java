package com.chiu.megalith.security.component.provider;

import com.chiu.megalith.security.user.LoginUser;
import com.chiu.megalith.common.lang.Const;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Component;

import java.util.Map;

/**
 * @author mingchiuli
 * @create 2022-12-30 10:57 am
 */

@Component
public final class EmailAuthenticationProvider extends ProviderSupport {

    private final StringRedisTemplate redisTemplate;

    @Value("${blog.email-try-count}")
    private int maxTryNum;

    public EmailAuthenticationProvider(StringRedisTemplate redisTemplate,
                                       UserDetailsService userDetailsService) {
        super(Const.GRANT_TYPE_EMAIL.getInfo(), userDetailsService);
        this.redisTemplate = redisTemplate;
    }


    @Override
    public void authProcess(LoginUser user,
                            UsernamePasswordAuthenticationToken authentication) {

        //username is login email
        String prefix = Const.EMAIL_KEY.getInfo() + user.getUsername();
        HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
        Map<String, String> entries = hashOperations.entries(prefix);

        if (entries.size() == 2) {
            String code = entries.get("code");
            String tryCount = entries.get("try_count");

            if (Integer.parseInt(tryCount) >= maxTryNum) {
                redisTemplate.delete(prefix);
                throw new BadCredentialsException("code reach max try number");
            }

            if (!code.equals(authentication.getCredentials().toString())) {
                redisTemplate.opsForHash().increment(prefix, "try_count", 1);
                throw new BadCredentialsException("code mismatch");
            }

            redisTemplate.delete(prefix);
        } else {
            throw new BadCredentialsException("code not exist");
        }
    }
}
