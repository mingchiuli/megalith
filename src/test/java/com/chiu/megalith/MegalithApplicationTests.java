package com.chiu.megalith;

import com.chiu.megalith.base.utils.JsonUtils;
import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.repository.UserRepository;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.time.LocalDateTime;
import java.util.*;

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
		userEntity.setRole("admin");
		userEntity.setCreated(LocalDateTime.now());
		userEntity.setStatus(0);
		userEntity.setAvatar("aa");
		userEntity.setLastLogin(LocalDateTime.now());
		userEntity.setPassword(passwordEncoder.encode("111111"));
		userEntity.setUsername("admin");

		userRepository.save(userEntity);
	}

	@Test
	void testRTNull() {
		Map<Object, Object> aaa = redisTemplate.opsForHash().entries("aaa");
		Set<String> keys = redisTemplate.keys("aa*");
		HashSet<String> strings = new HashSet<>();
		strings.add("aaa");
		strings.add("bbb");
		List<String> strings1 = redisTemplate.opsForValue().multiGet(strings);

		ArrayList<Object> objects = new ArrayList<>();
		objects.add(null);
		objects.add(null);


		List<Object> o = objects.stream().filter(Objects::nonNull).toList();
		System.out.println(aaa);


		List<String> loginFailureTimeStampRecords = redisTemplate.opsForList().range("redisKey", 0, -1);
		System.out.println(loginFailureTimeStampRecords);

	}

}
