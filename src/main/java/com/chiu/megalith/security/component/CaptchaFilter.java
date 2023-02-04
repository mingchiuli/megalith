package com.chiu.megalith.security.component;

import com.chiu.megalith.common.exception.CaptchaException;
import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.lang.Result;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.http.MediaType;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

@Component
@RequiredArgsConstructor
public class CaptchaFilter extends OncePerRequestFilter {

	private final ObjectMapper objectMapper;

	private final StringRedisTemplate redisTemplate;


	@Override
	protected void doFilterInternal(HttpServletRequest request,
									@NonNull HttpServletResponse response,
									@NonNull FilterChain filterChain) throws ServletException, IOException {

		String username = request.getParameter("username");
		if (StringUtils.hasLength(username) && !username.contains("@")) {
			// 校验验证码
			try {
				validate(request);
			} catch (CaptchaException e) {
				response.setContentType(MediaType.APPLICATION_JSON_VALUE);
				response.getWriter().write(
						objectMapper.writeValueAsString(
								Result.fail(400, e.getMessage())
						)
				);
				return;
			}
		}
		filterChain.doFilter(request, response);
	}


	private void validate(HttpServletRequest request) {
		String code = request.getParameter("code");
		String key = request.getParameter("key");

		if (!StringUtils.hasLength(code) || !StringUtils.hasLength(key)) {
			throw new CaptchaException("pin_code invalid");
		}
		if (!code.equals(redisTemplate.opsForValue().get(Const.CAPTCHA_KEY.getInfo() + key))) {
			redisTemplate.delete(Const.CAPTCHA_KEY.getInfo() + key);
			throw new CaptchaException("pin_code error");
		}
	}

}
