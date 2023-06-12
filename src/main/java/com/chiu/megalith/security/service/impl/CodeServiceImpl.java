package com.chiu.megalith.security.service.impl;

import com.chiu.megalith.security.service.CodeService;
import com.chiu.megalith.infra.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.stereotype.Service;
import com.chiu.megalith.infra.code.CodeFactory;


/**
 * @author mingchiuli
 * @create 2022-11-27 8:28 pm
 */
@Service
@RequiredArgsConstructor
public class CodeServiceImpl implements CodeService {

    private final CodeFactory codeFactory;

    private final JavaMailSender javaMailSender;

    private final StringRedisTemplate redisTemplate;

    @Value("${spring.mail.properties.from}")
    private String from;


    @Override
    public Boolean createEmailCode(String loginEmail) {
        String key = Const.EMAIL_KEY.getInfo() + loginEmail;
        boolean res = Boolean.FALSE.equals(redisTemplate.hasKey(key));
        if (res) {
            String code = codeFactory.create(Const.EMAIL_CODE.getInfo());
            codeFactory.save(code, key);
            var simpMsg = new SimpleMailMessage();
            simpMsg.setFrom(from);
            simpMsg.setTo(loginEmail);
            simpMsg.setSubject("login code");
            simpMsg.setText(code);
            javaMailSender.send(simpMsg);
        }
        return res;
    }
}
