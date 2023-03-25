package com.chiu.megalith.security.component;

import com.chiu.megalith.base.lang.Const;
import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.service.UserService;
import com.chiu.megalith.base.jwt.JwtUtils;
import com.chiu.megalith.base.lang.Result;
import com.chiu.megalith.security.user.LoginUser;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
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


@Component
@RequiredArgsConstructor
public class LoginSuccessHandler implements AuthenticationSuccessHandler {

	private final ObjectMapper objectMapper;

	private final JwtUtils jwtUtils;

	private final UserService userService;

	private final StringRedisTemplate redisTemplate;

	@Override
	public void onAuthenticationSuccess(HttpServletRequest request,
										HttpServletResponse response,
										Authentication authentication) throws IOException {
		response.setContentType(MediaType.APPLICATION_JSON_VALUE);
		ServletOutputStream outputStream = response.getOutputStream();
		String username = authentication.getName();

		LoginUser.loginUserCache.remove();
		redisTemplate.delete(Const.PASSWORD_KEY + username);

		UserEntity user = userService.retrieveUserInfo(username);
		// 生成jwt
		String jwt = jwtUtils.generateToken(String.valueOf(user.getId()),
				authentication
						.getAuthorities()
						.stream()
						.findFirst()
						.map(GrantedAuthority::getAuthority)
						.orElseThrow());

		userService.updateLoginTime(authentication.getName(), LocalDateTime.now());

		HashMap<String, Object> res = new HashMap<>(3);
		res.put("user", user);
		res.put("token", jwt);

		Result<Object> success = Result.success(res);

		outputStream.write(
				objectMapper.writeValueAsString(success).getBytes(StandardCharsets.UTF_8)
		);

		outputStream.flush();
		outputStream.close();
	}

}
