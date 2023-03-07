package com.chiu.megalith.security.config;

import com.chiu.megalith.security.component.provider.EmailAuthenticationProvider;
import com.chiu.megalith.security.component.provider.PasswordAuthenticationProvider;
import com.chiu.megalith.security.component.provider.PhoneAuthenticationProvider;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.ProviderManager;

import java.util.Arrays;
import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-11-27 5:56 pm
 */
@Configuration(proxyBeanMethods = false)
@RequiredArgsConstructor
public class AuthenticationManagerConfig {

    private final PasswordAuthenticationProvider passwordAuthenticationProvider;

    private final EmailAuthenticationProvider emailAuthenticationProvider;

    private final PhoneAuthenticationProvider phoneAuthenticationProvider;

    @Bean
    public AuthenticationManager authenticationManager() {
        List<AuthenticationProvider> providers = Arrays.asList(
                passwordAuthenticationProvider,
                emailAuthenticationProvider,
                phoneAuthenticationProvider
        );
        return new ProviderManager(providers, null);
    }
}
