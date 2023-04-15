package com.chiu.megalith.security.component;

import com.chiu.megalith.infra.lang.Const;
import com.chiu.megalith.manage.service.UserService;
import com.chiu.megalith.infra.jwt.JwtUtils;
import com.chiu.megalith.infra.lang.Result;
import com.chiu.megalith.security.user.LoginUser;
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
import java.util.HashMap;
import java.util.Map;


@Component
@RequiredArgsConstructor
public class LoginSuccessHandler implements AuthenticationSuccessHandler {

	private final ObjectMapper objectMapper;

	private final JwtUtils jwtUtils;

	private final UserService userService;

	private final StringRedisTemplate redisTemplate;

	@Value("${blog.jwt.access-token-expire}")
	private long accessExpire;

	@Value("${blog.jwt.refresh-token-expire}")
	private long refreshExpire;


	@Override
	public void onAuthenticationSuccess(HttpServletRequest request,
										HttpServletResponse response,
										Authentication authentication) throws IOException {
		response.setContentType(MediaType.APPLICATION_JSON_VALUE);
		ServletOutputStream outputStream = response.getOutputStream();
		String username = authentication.getName();
		LoginUser user = LoginUser.loginUserCache.get();
		LoginUser.loginUserCache.remove();
		redisTemplate.delete(Const.PASSWORD_KEY.getInfo() + username);

		userService.updateLoginTime(authentication.getName(), LocalDateTime.now());
		// 生成jwt
		String userId = user.getUserId().toString();
		String accessToken = jwtUtils.generateToken(
				userId,
				authentication.getAuthorities().stream()
						.findFirst()
						.map(GrantedAuthority::getAuthority)
						.orElseThrow(),
				accessExpire);

		String refreshToken = jwtUtils.generateToken(
				userId,
				"ROLE_REFRESH_TOKEN",
				refreshExpire);

		Map<String, Object> resp = new HashMap<>(5);
		resp.put("accessToken", accessToken);
		resp.put("refreshToken", refreshToken);

		outputStream.write(
				objectMapper.writeValueAsString(Result.success(resp))
						.getBytes(StandardCharsets.UTF_8)
		);

		outputStream.flush();
		outputStream.close();
	}

}
