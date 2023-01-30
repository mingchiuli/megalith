package com.chiu.megalith.auth.component.provider;

import com.chiu.megalith.auth.user.LoginUser;
import com.chiu.megalith.common.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.HashOperations;
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
public class EmailAuthenticationProvider extends DaoAuthenticationProvider implements ProviderSupport {

    private final StringRedisTemplate redisTemplate;

    @Value("${blog.email-try-count}")
    private int maxTryNum;

    @Override
    protected void additionalAuthenticationChecks(UserDetails userDetails,
                                                  UsernamePasswordAuthenticationToken authentication) {
        mainProcess(userDetails, authentication);
    }

    @Override
    public boolean supports(String grantType) {
        return Const.GRANT_TYPE_EMAIL.getInfo().equals(grantType);
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

    @Override
    public void mismatchProcess() {
        mismatchProcess(true);
    }

}
