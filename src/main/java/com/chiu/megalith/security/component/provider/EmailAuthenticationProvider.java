package com.chiu.megalith.security.component.provider;

import com.chiu.megalith.security.user.LoginUser;
import com.chiu.megalith.common.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;

import java.util.Map;

/**
 * @author mingchiuli
 * @create 2022-12-30 10:57 am
 */
@RequiredArgsConstructor
public class EmailAuthenticationProvider extends ProviderSupport {

    private final StringRedisTemplate redisTemplate;

    @Value("${blog.email-try-count}")
    private int maxTryNum;

    @Override
    public boolean supports(String grantType) {
        return Const.GRANT_TYPE_EMAIL.getInfo().equals(grantType);
    }

    @Override
    protected boolean lastProvider() {
        return true;
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
            String tryCount = entries.get("tryCount");

            if (Integer.parseInt(tryCount) >= maxTryNum) {
                redisTemplate.delete(prefix);
                throw new BadCredentialsException("code reach max try number");
            }

            if (!code.equalsIgnoreCase(authentication.getCredentials().toString())) {
                redisTemplate.opsForHash().increment(prefix, "tryCount", 1);
                throw new BadCredentialsException("code mismatch");
            }

            redisTemplate.delete(prefix);
        } else {
            throw new BadCredentialsException("code not exist");
        }
    }

}
