package com.chiu.megalith.security.service.impl;

import com.chiu.megalith.security.service.CodeService;
import com.chiu.megalith.infra.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.stereotype.Service;
import com.chiu.megalith.infra.operator.CodeOperator;


/**
 * @author mingchiuli
 * @create 2022-11-27 8:28 pm
 */
@Service
@RequiredArgsConstructor
public class CodeServiceImpl implements CodeService {

    private final CodeOperator codeOperator;

    private final JavaMailSender javaMailSender;

    @Value("${spring.mail.properties.from}")
    private String from;


    @Override
    public void createEmailCode(String loginEmail) {
        String code = codeOperator.createCode();
        codeOperator.saveCode(code, Const.EMAIL_KEY.getInfo() + loginEmail);
        SimpleMailMessage simpMsg = new SimpleMailMessage();
        simpMsg.setFrom(from);
        simpMsg.setTo(loginEmail);
        simpMsg.setSubject("login code");
        simpMsg.setText(code);
        javaMailSender.send(simpMsg);
    }
}
