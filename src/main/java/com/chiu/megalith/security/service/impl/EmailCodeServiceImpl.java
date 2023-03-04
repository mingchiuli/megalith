package com.chiu.megalith.security.service.impl;

import com.chiu.megalith.security.service.EmailCodeService;
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

import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;

/**
 * @author mingchiuli
 * @create 2022-11-27 8:28 pm
 */
@Service
@RequiredArgsConstructor
public class EmailCodeServiceImpl implements EmailCodeService {

    private static final char[] cs = {
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
            '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'
    };

    private final StringRedisTemplate redisTemplate;

    private final JavaMailSender javaMailSender;

    @Value("${spring.mail.properties.from}")
    private String from;


    @Override
    public void createEmailCode(String loginEmail) {
        String prefix = Const.EMAIL_KEY.getInfo() + loginEmail;
        String code = createText();

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

    private String createText() {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < 5; i++) {
            int idx = ThreadLocalRandom.current().nextInt(cs.length);
            builder.append(cs[idx]);
        }
        return builder.toString();
    }
}
