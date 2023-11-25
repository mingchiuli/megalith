package org.chiu.megalith.security.component.provider;

import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.utils.LuaScriptUtils;
import org.chiu.megalith.manage.repository.RoleRepository;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

import static org.chiu.megalith.infra.lang.ExceptionMessage.*;

/**
 * @author mingchiuli
 * @create 2023-03-08 1:59 am
 */
@Component
public final class SMSAuthenticationProvider extends ProviderParent {

    private final StringRedisTemplate redisTemplate;

    @Value("${blog.email-try-count}")
    private int maxTryNum;


    public SMSAuthenticationProvider(UserDetailsService userDetailsService,
                                     StringRedisTemplate redisTemplate,
                                     RoleRepository roleRepository) {
        super(Const.GRANT_TYPE_PHONE.getInfo(), userDetailsService, roleRepository);
        this.redisTemplate = redisTemplate;
    }

    @Override
    protected void authProcess(UserDetails user, UsernamePasswordAuthenticationToken authentication) {
        String prefix = Const.PHONE_KEY.getInfo() + user.getUsername();
        HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
        Map<String, String> entries = hashOperations.entries(prefix);

        if (!entries.isEmpty()) {
            String code = entries.get("sms");
            String tryCount = entries.get("try_count");

            if (Integer.parseInt(tryCount) >= maxTryNum) {
                redisTemplate.delete(prefix);
                throw new BadCredentialsException(SMS_TRY_MAX.getMsg());
            }

            if (!Objects.equals(code, authentication.getCredentials().toString())) {
                Long ttl = redisTemplate.execute(LuaScriptUtils.emailOrPhoneLua, Collections.singletonList(prefix), "try_count");
                if (Long.valueOf(0).equals(ttl)) {
                    throw new BadCredentialsException(SMS_EXPIRED.getMsg());
                }
                throw new BadCredentialsException(SMS_MISMATCH.getMsg());
            }

            redisTemplate.delete(prefix);
        } else {
            throw new BadCredentialsException(SMS_NOT_EXIST.getMsg());
        }
    }
}
