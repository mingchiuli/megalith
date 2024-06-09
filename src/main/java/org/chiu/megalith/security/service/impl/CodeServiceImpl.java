package org.chiu.megalith.security.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.SneakyThrows;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.utils.SmsUtils;
import org.chiu.megalith.user.repository.UserRepository;
import org.chiu.megalith.security.code.CodeFactory;
import org.chiu.megalith.security.http.SmsHttpService;
import org.chiu.megalith.security.service.CodeService;
import org.chiu.megalith.infra.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.stereotype.Service;
import org.chiu.megalith.infra.exception.CodeException;

import java.util.Collections;
import java.util.Map;

import static org.chiu.megalith.infra.lang.Const.SMS_CODE;
import static org.chiu.megalith.infra.lang.ExceptionMessage.*;


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

    private final UserRepository userRepository;

    private final SmsHttpService smsHttpService;

    private final SmsUtils smsUtils;

    private final ObjectMapper objectMapper;

    @Value("${spring.mail.properties.from}")
    private String from;


    @Override
    public void createEmailCode(String loginEmail) {
        userRepository.findByEmail(loginEmail)
                .orElseThrow(() -> new MissException(EMAIL_NOT_EXIST));
        String key = Const.EMAIL_KEY.getInfo() + loginEmail;
        boolean res = Boolean.FALSE.equals(redisTemplate.hasKey(key));
        if (res) {
            Object code = codeFactory.create(Const.EMAIL_CODE.getInfo());
            var simpMsg = new SimpleMailMessage();
            simpMsg.setFrom(from);
            simpMsg.setTo(loginEmail);
            simpMsg.setSubject("login code");
            simpMsg.setText(code.toString());
            javaMailSender.send(simpMsg);
            codeFactory.save(code, key);
            return;
        }
        throw new CodeException(CODE_EXISTED);
    }


    @SneakyThrows
    @Override
    public void createSMSCode(String loginSMS) {
        userRepository.findByPhone(loginSMS)
                .orElseThrow(() -> new MissException(SMS_NOT_EXIST));
        String key = Const.PHONE_KEY.getInfo() + loginSMS;
        boolean res = Boolean.FALSE.equals(redisTemplate.hasKey(key));
        if (res) {
            Object code = codeFactory.create(SMS_CODE.getInfo());
            Map<String, Object> codeMap = Collections.singletonMap("code", code);
            String signature = smsUtils.getSignature(loginSMS, objectMapper.writeValueAsString(codeMap));
            smsHttpService.sendSms("?Signature=" + signature);
            codeFactory.save(code, key);
            return;
        }
        throw new CodeException(CODE_EXISTED);
    }
}
