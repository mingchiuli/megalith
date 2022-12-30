package com.chiu.megalith.authentication.config;

import com.chiu.megalith.authentication.component.EmailAuthenticationProvider;
import com.chiu.megalith.manage.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.password.PasswordEncoder;

/**
 * @author mingchiuli
 * @create 2022-12-30 11:44 am
 */
@Configuration
@RequiredArgsConstructor
public class AuthenticationProviderConfig {

    private final UserDetailsService userDetailsService;

    private final PasswordEncoder passwordEncoder;

    private final StringRedisTemplate redisTemplate;

    private final UserRepository userRepository;



    @Bean
    public DaoAuthenticationProvider passwordAuthenticationProvider() {
        DaoAuthenticationProvider passwordAuthenticationProvider = new DaoAuthenticationProvider();
        passwordAuthenticationProvider.setUserDetailsService(userDetailsService);
        passwordAuthenticationProvider.setPasswordEncoder(passwordEncoder);
        return passwordAuthenticationProvider;
    }

    @Bean
    public EmailAuthenticationProvider emailAuthenticationProvider() {
        EmailAuthenticationProvider emailAuthenticationProvider = new EmailAuthenticationProvider(redisTemplate, userRepository);
        emailAuthenticationProvider.setUserDetailsService(userDetailsService);
        return emailAuthenticationProvider;
    }
}
