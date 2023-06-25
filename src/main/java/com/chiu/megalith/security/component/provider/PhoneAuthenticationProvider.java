package com.chiu.megalith.security.component.provider;

import com.chiu.megalith.infra.lang.Const;
import com.chiu.megalith.infra.utils.LuaScriptUtils;
import com.chiu.megalith.security.user.LoginUser;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

/**
 * @author mingchiuli
 * @create 2023-03-08 1:59 am
 */
@Component
public final class PhoneAuthenticationProvider extends ProviderSupport {

    private final StringRedisTemplate redisTemplate;

    @Value("${blog.email-try-count}")
    private int maxTryNum;


    public PhoneAuthenticationProvider(UserDetailsService userDetailsService,
                                       StringRedisTemplate redisTemplate) {
        super(Const.GRANT_TYPE_PHONE.getInfo(), userDetailsService);
        this.redisTemplate = redisTemplate;
    }

    @Override
    protected void authProcess(LoginUser user, UsernamePasswordAuthenticationToken authentication) {
        String prefix = Const.PHONE_KEY.getInfo() + user.getUsername();
        HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
        Map<String, String> entries = hashOperations.entries(prefix);

        if (Boolean.FALSE.equals(entries.isEmpty())) {
            String code = entries.get("sms");
            String tryCount = entries.get("try_count");

            if (Integer.parseInt(tryCount) >= maxTryNum) {
                redisTemplate.delete(prefix);
                throw new BadCredentialsException("sms reach max try number");
            }

            if (Boolean.FALSE.equals(Objects.equals(code, authentication.getCredentials().toString()))) {
                Long ttl = redisTemplate.execute(LuaScriptUtils.emailOrPhoneLua, Collections.singletonList(prefix), "try_count");
                if (ttl == 0) {
                    throw new BadCredentialsException("sms expired");
                }
                throw new BadCredentialsException("sms mismatch");
            }

            redisTemplate.delete(prefix);
        } else {
            throw new BadCredentialsException("sms not exist");
        }
    }
}
