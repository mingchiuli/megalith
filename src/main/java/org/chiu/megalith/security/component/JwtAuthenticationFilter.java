package org.chiu.megalith.security.component;

import com.auth0.jwt.exceptions.JWTVerificationException;
import com.auth0.jwt.interfaces.DecodedJWT;
import org.chiu.megalith.infra.jwt.JwtUtils;
import org.chiu.megalith.infra.lang.Result;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.preauth.PreAuthenticatedAuthenticationToken;
import org.springframework.security.web.authentication.www.BasicAuthenticationFilter;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.io.IOException;

import static org.chiu.megalith.infra.lang.Const.ROLE_PREFIX;
import static org.chiu.megalith.infra.lang.Const.TOKEN_PREFIX;
import static org.chiu.megalith.infra.lang.ExceptionMessage.TOKEN_INVALID;

@Component
public class JwtAuthenticationFilter extends BasicAuthenticationFilter {

    private final ObjectMapper objectMapper;

    private final JwtUtils jwtUtils;

    public JwtAuthenticationFilter(AuthenticationManager authenticationManager, ObjectMapper objectMapper,
            JwtUtils jwtUtils) {
        super(authenticationManager);
        this.objectMapper = objectMapper;
        this.jwtUtils = jwtUtils;
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
        } catch (JWTVerificationException e) {
            response.setContentType(MediaType.APPLICATION_JSON_VALUE);
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

    private Authentication getAuthentication(String token) {
        String jwt;
        try {
            jwt = token.substring(TOKEN_PREFIX.getInfo().length());
        } catch (IndexOutOfBoundsException e) {
            throw new JWTVerificationException(TOKEN_INVALID.getMsg());
        }

        DecodedJWT decodedJWT = jwtUtils.getJWTVerifierByToken(jwt);
        String userId = decodedJWT.getSubject();
        String role = decodedJWT.getClaim("role").asString();
        return new PreAuthenticatedAuthenticationToken(userId,
                null,
                AuthorityUtils.createAuthorityList(role));
    }
}
