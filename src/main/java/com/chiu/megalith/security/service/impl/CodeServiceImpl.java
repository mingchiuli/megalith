package com.chiu.megalith.security.service.impl;

import com.chiu.megalith.security.service.CodeService;
import com.chiu.megalith.common.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.SessionCallback;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.lang.NonNull;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.stereotype.Service;
import com.chiu.megalith.common.generator.CodeGenerator;

import java.util.*;
import java.util.concurrent.TimeUnit;

/**
 * @author mingchiuli
 * @create 2022-11-27 8:28 pm
 */
@Service
@RequiredArgsConstructor
public class CodeServiceImpl implements CodeService {

    private final StringRedisTemplate redisTemplate;

    private final JavaMailSender javaMailSender;

    @Value("${spring.mail.properties.from}")
    private String from;


    @Override
    public void createEmailCode(String loginEmail) {
        String prefix = Const.EMAIL_KEY.getInfo() + loginEmail;
        String code = CodeGenerator.createText();

        Map<String, Object> map = new HashMap<>(3);
        map.put("code", code);
        map.put("try_count", "0");

        redisTemplate.execute(new SessionCallback<>() {
            @Override
            @SuppressWarnings("unchecked")
            public List<Object> execute(@NonNull RedisOperations operations) throws DataAccessException {
                operations.multi();
                operations.opsForHash().putAll(prefix, map);
                operations.expire(prefix, 120, TimeUnit.SECONDS);
                return operations.exec();
            }
        });

        SimpleMailMessage simpMsg = new SimpleMailMessage();
        simpMsg.setFrom(from);
        simpMsg.setTo(loginEmail);
        simpMsg.setSubject("login code");
        simpMsg.setText(code);
        javaMailSender.send(simpMsg);
    }
}
