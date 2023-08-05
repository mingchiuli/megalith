package org.chiu.megalith.security.component;

import org.chiu.megalith.infra.lang.Result;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.stereotype.Component;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

/**
 * 如果访问白名单没有配置的url
 * 和SecurityContextHolder.getContext().setAuthentication(token);
 * 就会跳转到这个处理逻辑
 */
@Component
@RequiredArgsConstructor
public class JwtAuthenticationEntryPoint implements AuthenticationEntryPoint {

	private final ObjectMapper objectMapper;

	@Override
	public void commence(HttpServletRequest request, HttpServletResponse response, AuthenticationException authException) throws IOException {
		response.setContentType(MediaType.APPLICATION_JSON_VALUE);
		response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
		ServletOutputStream outputStream = response.getOutputStream();
		Result<String> result = Result.fail(authException.getMessage());
		outputStream.write(
				objectMapper.writeValueAsString(result)
						.getBytes(StandardCharsets.UTF_8)
		);

		outputStream.flush();
		outputStream.close();
	}
}
