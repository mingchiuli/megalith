package com.chiu.megalith.auth.component;

import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.service.UserService;
import com.chiu.megalith.auth.dto.LoginSuccessDto;
import com.chiu.megalith.common.jwt.JwtUtils;
import com.chiu.megalith.common.lang.Result;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;



@Component
@RequiredArgsConstructor
public class LoginSuccessHandler implements AuthenticationSuccessHandler {

	private final ObjectMapper objectMapper;

	private final JwtUtils jwtUtils;

	private final UserService userService;


	@Override
	public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response, Authentication authentication) throws IOException {
		response.setContentType(MediaType.APPLICATION_JSON_VALUE);
		ServletOutputStream outputStream = response.getOutputStream();

		String username = authentication.getName();

		UserEntity user = userService.retrieveUserInfo(username);

		// 生成jwt
		String jwt = jwtUtils.generateToken(String.valueOf(user.getId()),
				authentication.getAuthorities().
						stream().
						findFirst().
						map(GrantedAuthority::getAuthority).
						orElse("ROLE_default"));

		userService.updateLoginTime(authentication.getName(), LocalDateTime.now());

		Result<LoginSuccessDto> success = Result.success(
				LoginSuccessDto.
						builder().
						user(user).
						token(jwt).
						build()
		);

		outputStream.write(
				objectMapper.writeValueAsString(success).getBytes(StandardCharsets.UTF_8)
		);

		outputStream.flush();
		outputStream.close();
	}

}
