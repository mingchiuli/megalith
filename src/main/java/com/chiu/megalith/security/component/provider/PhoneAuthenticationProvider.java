package com.chiu.megalith.security.component.provider;

import com.chiu.megalith.base.lang.Const;
import com.chiu.megalith.security.user.LoginUser;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.Map;

/**
 * @author mingchiuli
 * @create 2023-03-08 1:59 am
 */
@Component
public final class PhoneAuthenticationProvider extends ProviderSupport {

    private final StringRedisTemplate redisTemplate;

    @Value("${blog.email-try-count}")
    private int maxTryNum;


    public PhoneAuthenticationProvider(UserDetailsService userDetailsService, StringRedisTemplate redisTemplate) {
        super(Const.GRANT_TYPE_PHONE.getInfo(), userDetailsService);
        this.redisTemplate = redisTemplate;
    }

    @Override
    protected void authProcess(LoginUser user, UsernamePasswordAuthenticationToken authentication) {
        String prefix = Const.PASSWORD_KEY.getInfo() + user.getUsername();
        HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
        Map<String, String> entries = hashOperations.entries(prefix);

        if (!entries.isEmpty()) {
            String code = entries.get("code");
            String tryCount = entries.get("try_count");

            if (Integer.parseInt(tryCount) >= maxTryNum) {
                redisTemplate.delete(prefix);
                throw new BadCredentialsException("code reach max try number");
            }

            if (!code.equals(authentication.getCredentials().toString())) {

                String lua = "local ttl =  redis.call('ttl', KEYS[1]);" +
                        "if (ttl == -2) then return 0 end;" +
                        "redis.call('hincrby', KEYS[1], ARGV[1], 1);" +
                        "redis.call('expire', KEYS[1], ttl);" +
                        "return ttl;";

                RedisScript<Long> script = RedisScript.of(lua, Long.class);
                Long ttl = redisTemplate.execute(script, Collections.singletonList(prefix), "try_count");
                if (ttl == 0) {
                    throw new BadCredentialsException("code expired");
                }
                throw new BadCredentialsException("code mismatch");
            }

            redisTemplate.delete(prefix);
        } else {
            throw new BadCredentialsException("code not exist");
        }
    }
}
