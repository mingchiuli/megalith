package com.chiu.megalith.captcha.service.impl;

import com.chiu.megalith.captcha.dto.CaptchaDto;
import com.chiu.megalith.captcha.service.CaptchaService;
import com.chiu.megalith.common.lang.Const;
import com.google.code.kaptcha.Producer;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

/**
 * @author mingchiuli
 * @create 2022-11-27 8:28 pm
 */
@Service
@RequiredArgsConstructor
public class CaptchaServiceImpl implements CaptchaService {
    private final Producer producer;
    private final StringRedisTemplate redisTemplate;

    @SneakyThrows
    @Override
    public CaptchaDto createCaptcha() {
        String key = UUID.randomUUID().toString();
        String code = producer.createText();

        BufferedImage image = producer.createImage(code);
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ImageIO.write(image, "jpg", outputStream);

        Base64.Encoder encoder = Base64.getEncoder();
        String str = "data:image/jpeg;base64,";

        String base64Img = str + encoder.encodeToString(outputStream.toByteArray());

        redisTemplate.opsForValue().set(Const.CAPTCHA_KEY.getMsg() + key, code, 120, TimeUnit.SECONDS);


        return CaptchaDto.
                builder().
                key(key).
                captchaImg(base64Img).
                build();
    }
}
