package org.chiu.megalith.security.component.provider;

import org.chiu.megalith.infra.utils.LuaScriptUtils;
import org.chiu.megalith.user.repository.RoleRepository;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.security.component.token.EmailAuthenticationToken;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Component;

import java.util.Collections;

import static org.chiu.megalith.infra.lang.ExceptionMessage.*;

/**
 * @author mingchiuli
 * @create 2022-12-30 10:57 am
 */

@Component
public final class EmailAuthenticationProvider extends ProviderBase {

    private final StringRedisTemplate redisTemplate;

    @Value("${blog.email-try-count}")
    private int maxTryNum;

    public EmailAuthenticationProvider(StringRedisTemplate redisTemplate,
                                       UserDetailsService userDetailsService,
                                       RoleRepository roleRepository) {
        super(userDetailsService, roleRepository);
        this.redisTemplate = redisTemplate;
    }

    @Override
    public boolean supports(Class<?> authentication) {
        return EmailAuthenticationToken.class.equals(authentication);
    }

    @Override
    public void authProcess(UserDetails user, Authentication authentication) {
        //username is login email
        String prefix = Const.EMAIL_KEY.getInfo() + user.getUsername();
        HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
        var entries = hashOperations.entries(prefix);

        if (!entries.isEmpty()) {
            String code = entries.get("code");
            String tryCount = entries.get("try_count");

            if (Integer.parseInt(tryCount) >= maxTryNum) {
                redisTemplate.delete(prefix);
                throw new BadCredentialsException(CODE_TRY_MAX.getMsg());
            }

            if (Boolean.FALSE.equals(code.equalsIgnoreCase(authentication.getCredentials().toString()))) {
                Long ttl = redisTemplate.execute(LuaScriptUtils.emailOrPhoneLua, Collections.singletonList(prefix), "try_count");
                if (Long.valueOf(0).equals(ttl)) {
                    throw new BadCredentialsException(CODE_EXPIRED.getMsg());
                }
                throw new BadCredentialsException(CODE_MISMATCH.getMsg());
            }

            redisTemplate.delete(prefix);
            return;
        }

        throw new BadCredentialsException(CODE_NOT_EXIST.getMsg());

    }
}
