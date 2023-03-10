package com.chiu.megalith.security.component;

import com.chiu.megalith.base.jwt.JwtUtils;
import com.chiu.megalith.base.lang.Result;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwtException;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpHeaders;
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
		if (!StringUtils.hasLength(jwt)) {
			chain.doFilter(request, response);
			return;
		}

		Authentication authentication;

		try {
			authentication = getAuthentication(jwt);
		} catch (JwtException e) {
			response.setContentType(MediaType.APPLICATION_JSON_VALUE);
			response.getWriter().write(
					objectMapper.writeValueAsString(
									Result.fail(401, e.getMessage())
					)
			);
			return;
		}

		//?????????????????????????????????????????????????????????set???????????????
		SecurityContextHolder.getContext().setAuthentication(authentication);
		chain.doFilter(request, response);
	}

	private Authentication getAuthentication(String jwt) {
		Claims claim = jwtUtils.getClaimByToken(jwt).
				orElseThrow(() -> new JwtException("token invalid"));

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
