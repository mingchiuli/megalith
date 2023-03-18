package com.chiu.megalith.security.component;

import com.chiu.megalith.base.lang.Result;
import com.chiu.megalith.security.user.LoginUser;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.AuthenticationFailureHandler;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.charset.StandardCharsets;

@Component
@RequiredArgsConstructor
public class LoginFailureHandler implements AuthenticationFailureHandler {
	private final ObjectMapper objectMapper;

	@Override
	public void onAuthenticationFailure(HttpServletRequest request,
										HttpServletResponse response,
										AuthenticationException exception) throws IOException {
		String username = request.getParameter("username");
		LoginUser.loginUserCache.remove(username);

		response.setContentType(MediaType.APPLICATION_JSON_VALUE);
		ServletOutputStream outputStream = response.getOutputStream();
		Result<Object> result = Result.fail(401, exception.getMessage());
		outputStream.write(
				objectMapper.writeValueAsString(result).getBytes(StandardCharsets.UTF_8)
		);

		outputStream.flush();
		outputStream.close();
	}
}
