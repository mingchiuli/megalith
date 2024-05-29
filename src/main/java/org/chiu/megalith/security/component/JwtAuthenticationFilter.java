package org.chiu.megalith.security.component;

import org.chiu.megalith.security.token.Claims;
import org.chiu.megalith.security.token.TokenUtils;
import org.chiu.megalith.infra.lang.Result;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.jwk.source.JWKSetParseException;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.chiu.megalith.infra.utils.SecurityAuthenticationUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.www.BasicAuthenticationFilter;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.chiu.megalith.infra.lang.Const.*;
import static org.chiu.megalith.infra.lang.ExceptionMessage.*;

@Component
public class JwtAuthenticationFilter extends BasicAuthenticationFilter {

    private final ObjectMapper objectMapper;

    private final TokenUtils<Claims> tokenUtils;

    private final SecurityAuthenticationUtils securityAuthenticationUtils;

    private final StringRedisTemplate redisTemplate;

    public JwtAuthenticationFilter(AuthenticationManager authenticationManager,
                                   ObjectMapper objectMapper,
                                   TokenUtils<Claims> tokenUtils,
                                   SecurityAuthenticationUtils securityAuthenticationUtils,
                                   StringRedisTemplate redisTemplate) {
        super(authenticationManager);
        this.objectMapper = objectMapper;
        this.tokenUtils = tokenUtils;
        this.securityAuthenticationUtils = securityAuthenticationUtils;
        this.redisTemplate = redisTemplate;
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain chain)
            throws IOException, ServletException {

        String jwt = request.getHeader(HttpHeaders.AUTHORIZATION);
        if (!StringUtils.hasLength(jwt)) {
            chain.doFilter(request, response);
            return;
        }

        Authentication authentication;

        try {
            authentication = getAuthentication(jwt);
        } catch (JWKSetParseException e) {
            response.setContentType(MediaType.APPLICATION_JSON_VALUE);
            response.setCharacterEncoding(StandardCharsets.UTF_8.name());
            response.setStatus(HttpStatus.UNAUTHORIZED.value());
            response.getWriter().write(
                    objectMapper.writeValueAsString(
                            Result.fail(e.getMessage())));
            return;
        }

        // 非白名单资源、接口都要走这个流程，没有set就不能访问
        SecurityContextHolder.getContext().setAuthentication(authentication);
        chain.doFilter(request, response);
    }

    private Authentication getAuthentication(String token) throws JWKSetParseException {
        String jwt;
        try {
            jwt = token.substring(TOKEN_PREFIX.getInfo().length());
        } catch (IndexOutOfBoundsException e) {
            throw new JWKSetParseException(TOKEN_INVALID.getMsg(), e);
        }

        Claims claims = tokenUtils.getVerifierByToken(jwt);
        String userId = claims.getUserId();
        List<String> roles = claims.getRoles();

        String mark = redisTemplate.opsForValue().get(BLOCK_USER.getInfo() + userId);

        if (StringUtils.hasLength(mark)) {
            throw new JWKSetParseException(RE_LOGIN.getMsg(), null);
        }

        return securityAuthenticationUtils.getAuthentication(roles, userId);
    }
}
