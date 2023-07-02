package com.chiu.megalith.security.component;

import com.chiu.megalith.infra.jwt.JwtUtils;
import com.chiu.megalith.infra.lang.Result;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwtException;
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

@Component
public class JwtAuthenticationFilter extends BasicAuthenticationFilter {

	private final ObjectMapper objectMapper;

	private final JwtUtils jwtUtils;

	public JwtAuthenticationFilter(AuthenticationManager authenticationManager,
								   ObjectMapper objectMapper,
								   JwtUtils jwtUtils) {
		super(authenticationManager);
		this.objectMapper = objectMapper;
		this.jwtUtils = jwtUtils;
	}


	@Override
	protected void doFilterInternal(HttpServletRequest request,
									HttpServletResponse response,
									FilterChain chain) throws IOException, ServletException {

		String jwt = request.getHeader(HttpHeaders.AUTHORIZATION);
		if (Boolean.FALSE.equals(StringUtils.hasLength(jwt))) {
			chain.doFilter(request, response);
			return;
		}

		Authentication authentication;

		try {
			authentication = getAuthentication(jwt);
		} catch (JwtException e) {
			response.setContentType(MediaType.APPLICATION_JSON_VALUE);
            response.setStatus(HttpStatus.UNAUTHORIZED.value());
			response.getWriter().write(
					objectMapper.writeValueAsString(
									Result.fail(e.getMessage()))
			);
			return;
		}

		//非白名单资源、接口都要走这个流程，没有set就不能访问
		SecurityContextHolder.getContext().setAuthentication(authentication);
		chain.doFilter(request, response);
	}

	private Authentication getAuthentication(String jwt) {
		Claims claim = jwtUtils.getClaimByToken(jwt)
				.orElseThrow(() -> new JwtException("token invalid"));

		if (jwtUtils.isTokenExpired(claim.getExpiration())) {
			throw new JwtException("token expired");
		}

		String userId = claim.getSubject();
		String role = (String) claim.get("role");
		return new PreAuthenticatedAuthenticationToken(userId,
				null,
				AuthorityUtils.createAuthorityList(role));
	}
}
