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


@Configuration
@RequiredArgsConstructor
public class SecurityConfig {
    
    private final LoginFailureHandler loginFailureHandler;

    private final LoginSuccessHandler loginSuccessHandler;

    private final JwtAuthenticationEntryPoint jwtAuthenticationEntryPoint;

    private final AuthenticationManager authenticationManager;
        
    private final JwtAuthenticationFilter jwtAuthenticationFilter;

    private static final String[] URL_WHITELIST = {
            "/code/**",
            "/public/blog/**",
            "/search/public/**",
            "/edit/ws/**",
            "/actuator/**",
            "/sys/user/register/**"
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
                .logout(AbstractHttpConfigurer::disable)
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
                .addFilter(jwtAuthenticationFilter)
                .authenticationManager(authenticationManager)
                .build();
    }

}
