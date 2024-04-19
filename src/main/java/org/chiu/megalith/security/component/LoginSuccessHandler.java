package org.chiu.megalith.security.component;

import com.auth0.jwt.interfaces.DecodedJWT;
import org.chiu.megalith.security.token.TokenUtils;
import org.chiu.megalith.manage.service.UserService;
import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.security.user.LoginUser;
import org.chiu.megalith.security.vo.LoginSuccessVo;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.http.MediaType;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;

import static org.chiu.megalith.infra.lang.Const.*;


@Component
@RequiredArgsConstructor
public class LoginSuccessHandler implements AuthenticationSuccessHandler {

	private final ObjectMapper objectMapper;

	private final TokenUtils<DecodedJWT> tokenUtils;

	private final UserService userService;

	private final StringRedisTemplate redisTemplate;

	@Value("${blog.jwt.access-token-expire}")
	private long accessExpire;

	@Value("${blog.jwt.refresh-token-expire}")
	private long refreshExpire;


	@Override
	public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response, Authentication authentication) throws IOException {
		response.setContentType(MediaType.APPLICATION_JSON_VALUE);
		ServletOutputStream outputStream = response.getOutputStream();
		String username = authentication.getName();
		redisTemplate.delete(PASSWORD_KEY.getInfo() + username);

		userService.updateLoginTime(username, LocalDateTime.now());
		// 生成jwt
		LoginUser user = (LoginUser) authentication.getPrincipal();
		Long userId = user.getUserId();
		String accessToken = tokenUtils.generateToken(userId.toString(),
				authentication.getAuthorities().stream()
						.findFirst()
						.map(GrantedAuthority::getAuthority)
						.orElseThrow(),
				accessExpire);

		String refreshToken = tokenUtils.generateToken(userId.toString(),
				ROLE_PREFIX.getInfo() + "REFRESH_TOKEN",
				refreshExpire);

		outputStream.write(
				objectMapper.writeValueAsString(
						Result.success(
								LoginSuccessVo.builder()
										.accessToken(TOKEN_PREFIX.getInfo() + accessToken)
										.refreshToken(TOKEN_PREFIX.getInfo() + refreshToken)
										.build())
						)
						.getBytes(StandardCharsets.UTF_8)
		);

		outputStream.flush();
		outputStream.close();
	}

}
