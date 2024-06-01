package org.chiu.megalith;

import org.chiu.megalith.infra.utils.JsonUtils;
import org.chiu.megalith.user.entity.UserEntity;
import org.chiu.megalith.user.repository.UserRepository;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.time.LocalDateTime;

@SpringBootTest
class MegalithApplicationTests {

	@Autowired
	UserRepository userRepository;

	@Autowired
	PasswordEncoder passwordEncoder;

	@Autowired
	StringRedisTemplate redisTemplate;

	@Autowired
	JsonUtils jsonUtils;

	@Test
	void contextLoads() {
		UserEntity userEntity = new UserEntity();
		userEntity.setEmail("1111@163.com");
		userEntity.setCreated(LocalDateTime.now());
		userEntity.setStatus(0);
		userEntity.setAvatar("aa");
		userEntity.setLastLogin(LocalDateTime.now());
		userEntity.setPassword(passwordEncoder.encode("111111"));
		userEntity.setUsername("admin");

	}
}
