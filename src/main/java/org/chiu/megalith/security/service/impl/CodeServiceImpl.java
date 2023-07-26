package org.chiu.megalith.security.service.impl;

import org.chiu.megalith.security.service.CodeService;
import org.chiu.megalith.infra.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.stereotype.Service;
import org.chiu.megalith.infra.code.CodeFactory;
import org.chiu.megalith.infra.exception.CodeException;


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
    public void createEmailCode(String loginEmail) {
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
            return;
        }
        throw new CodeException("code existed");
    }


    @Override
    public void createSMSCode(String loginSMS) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'createSMSCode'");
    }
}