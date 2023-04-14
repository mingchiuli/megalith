package com.chiu.megalith.security.config;

import com.chiu.megalith.infra.utils.SpringUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
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
@DependsOn(value = {"springUtils",
        "emailAuthenticationProvider", "passwordAuthenticationProvider", "phoneAuthenticationProvider"})
public class AuthenticationManagerConfig {

    @Bean
    public AuthenticationManager authenticationManager() {
        List<AuthenticationProvider> providers = SpringUtils.getBeans(AuthenticationProvider.class);
        return new ProviderManager(providers, null);
    }
}
