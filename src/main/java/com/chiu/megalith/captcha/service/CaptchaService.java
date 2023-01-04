package com.chiu.megalith.captcha.service;

import com.chiu.megalith.captcha.dto.CaptchaDto;


/**
 * @author mingchiuli
 * @create 2022-11-27 8:27 pm
 */
public interface CaptchaService {

    CaptchaDto createCaptcha();

    void createEmailCode(String loginName);

}
