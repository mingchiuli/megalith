package org.chiu.megalith.security.config;


import org.chiu.megalith.security.component.*;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.logout.LogoutFilter;


@Configuration(proxyBeanMethods = false)
@RequiredArgsConstructor
public class SecurityConfig {
    
    private final LoginFailureHandler loginFailureHandler;

    private final LoginSuccessHandler loginSuccessHandler;

    private final JwtAuthenticationEntryPoint jwtAuthenticationEntryPoint;

    private final JwtLogoutSuccessHandler jwtLogoutSuccessHandler;

    private final JwtAuthenticationFilter jwtAuthenticationFilter;

    private final AuthenticationManager authenticationManager;

    private static final String[] URL_WHITELIST = {
            "/code/**",
            "/public/blog/**",
            "/search/website/query/*",
            "/search/public/**",
            "/log/**",
            "/edit/**",
            "/actuator/**"
    };

    @Bean
    SecurityFilterChain configure(HttpSecurity http) throws Exception {
        return http
                .cors(Customizer.withDefaults())
                .csrf(AbstractHttpConfigurer::disable)
                .formLogin(formLogin ->
                        formLogin
                                .successHandler(loginSuccessHandler)
                                .failureHandler(loginFailureHandler))
                .logout(logout ->
                        logout
                                .logoutSuccessHandler(jwtLogoutSuccessHandler))
                .sessionManagement(sessionManagement ->
                        sessionManagement
                                .sessionCreationPolicy(SessionCreationPolicy.STATELESS))
                .authorizeHttpRequests(authorizeHttpRequests ->
                        authorizeHttpRequests
                                .requestMatchers(URL_WHITELIST)
                                .permitAll()
                                .anyRequest()
                                .authenticated())
                .exceptionHandling(exceptionHandling ->
                        exceptionHandling
                                .authenticationEntryPoint(jwtAuthenticationEntryPoint))
                .addFilterBefore(jwtAuthenticationFilter, LogoutFilter.class)
                .authenticationManager(authenticationManager)
                .build();
    }

}
