package com.chiu.megalith.security.config;

import com.chiu.megalith.manage.service.UserService;
import com.chiu.megalith.security.component.provider.EmailAuthenticationProvider;
import com.chiu.megalith.security.component.provider.PasswordAuthenticationProvider;
import com.chiu.megalith.security.component.provider.PhoneAuthenticationProvider;
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

    private final UserService userService;

    private final UserDetailsService userDetailsService;

    private final StringRedisTemplate redisTemplate;

    private final PasswordEncoder passwordEncoder;


    @Bean
    public PasswordAuthenticationProvider passwordAuthenticationProvider() {
        PasswordAuthenticationProvider passwordAuthenticationProvider = new PasswordAuthenticationProvider(passwordEncoder, redisTemplate, userService);
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

    @Bean
    public PhoneAuthenticationProvider phoneAuthenticationProvider() {
        PhoneAuthenticationProvider phoneAuthenticationProvider = new PhoneAuthenticationProvider();
        phoneAuthenticationProvider.setUserDetailsService(userDetailsService);
        phoneAuthenticationProvider.setHideUserNotFoundExceptions(false);
        return phoneAuthenticationProvider;
    }
}
