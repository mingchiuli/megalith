package com.chiu.megalith.auth.config;

import com.chiu.megalith.auth.component.provider.EmailAuthenticationProvider;
import com.chiu.megalith.auth.component.provider.PasswordAuthenticationProvider;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.password.PasswordEncoder;

/**
 * @author mingchiuli
 * @create 2022-12-30 11:44 am
 */
@Configuration(proxyBeanMethods = false)
@RequiredArgsConstructor
public class AuthenticationProviderConfig {

    private final UserDetailsService userDetailsService;

    private final StringRedisTemplate redisTemplate;

    private final PasswordEncoder passwordEncoder;


    @Bean
    public PasswordAuthenticationProvider passwordAuthenticationProvider() {
        PasswordAuthenticationProvider passwordAuthenticationProvider = new PasswordAuthenticationProvider(passwordEncoder);
        passwordAuthenticationProvider.setUserDetailsService(userDetailsService);
        passwordAuthenticationProvider.setHideUserNotFoundExceptions(false);
        return passwordAuthenticationProvider;
    }

    @Bean
    public EmailAuthenticationProvider emailAuthenticationProvider() {
        EmailAuthenticationProvider emailAuthenticationProvider = new EmailAuthenticationProvider(redisTemplate);
        emailAuthenticationProvider.setUserDetailsService(userDetailsService);
        emailAuthenticationProvider.setHideUserNotFoundExceptions(false);
        return emailAuthenticationProvider;
    }
}
