package com.chiu.megalith.auth.config;


import com.chiu.megalith.auth.component.*;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.authentication.logout.LogoutFilter;


@Configuration(proxyBeanMethods = false)
@EnableMethodSecurity
@RequiredArgsConstructor
public class SecurityConfig {
    private final LoginFailureHandler loginFailureHandler;

    private final LoginSuccessHandler loginSuccessHandler;

    private final CaptchaFilter captchaFilter;

    private final JwtAuthenticationEntryPoint jwtAuthenticationEntryPoint;

    private final JwtLogoutSuccessHandler jwtLogoutSuccessHandler;

    private final JwtAuthenticationFilter jwtAuthenticationFilter;

    private final AuthenticationManager authenticationManager;

    private static final String[] URL_WHITELIST = {
            "/captcha/**",
            "/public/blog/**",
            "/search/website/*",
            "/search/blog/**",
            "/coop/**",
            "/log/**"
    };

    @Bean
    public SecurityFilterChain configure(HttpSecurity http) throws Exception {

        return http.cors().
                and().
                csrf().
                disable().

                //登录配置
                formLogin().
                successHandler(loginSuccessHandler).
                failureHandler(loginFailureHandler).

                and().
                logout().
                logoutSuccessHandler(jwtLogoutSuccessHandler).

                //禁用session
                and().
                sessionManagement().
                sessionCreationPolicy(SessionCreationPolicy.STATELESS).

                //配置拦截规则
                and().
                authorizeHttpRequests().
                requestMatchers(URL_WHITELIST).permitAll().
                anyRequest().authenticated().

                //异常处理器
                and().
                exceptionHandling().
                authenticationEntryPoint(jwtAuthenticationEntryPoint).

                //配置自定义的过滤器
                and().
                addFilterBefore(captchaFilter, UsernamePasswordAuthenticationFilter.class).
                addFilterBefore(jwtAuthenticationFilter, LogoutFilter.class).
                authenticationManager(authenticationManager).
                build();
    }

}
