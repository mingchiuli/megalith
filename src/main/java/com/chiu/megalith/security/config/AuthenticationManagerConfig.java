package com.chiu.megalith.security.config;

import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.ProviderManager;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-11-27 5:56 pm
 */
@Configuration(proxyBeanMethods = false)
@RequiredArgsConstructor
public class AuthenticationManagerConfig {

    private final List<AuthenticationProvider> providers;

    @Bean
    AuthenticationManager authenticationManager() {
        return new ProviderManager(providers, null);
    }
}
