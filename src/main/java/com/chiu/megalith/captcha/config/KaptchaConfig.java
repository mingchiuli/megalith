package com.chiu.megalith.captcha.config;

import com.google.code.kaptcha.impl.DefaultKaptcha;
import com.google.code.kaptcha.util.Config;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Properties;

/**
 * @author mingchiuli
 * @create 2021-12-14 11:52 AM
 */
@Configuration
public class KaptchaConfig {

    @Bean
    DefaultKaptcha producer(@Value("${kaptcha.border}") String border,
                            @Value("${kaptcha.textproducer.font.color}") String color,
                            @Value("${kaptcha.textproducer.char.space}") String space,
                            @Value("${kaptcha.image.height}") String height,
                            @Value("${kaptcha.image.width}") String width,
                            @Value("${kaptcha.textproducer.font.size}") String fontSize) {
        Properties properties = new Properties();
        properties.put("kaptcha.border", border);
        properties.put("kaptcha.textproducer.font.color", color);
        properties.put("kaptcha.textproducer.char.space", space);
        properties.put("kaptcha.image.height", height);
        properties.put("kaptcha.image.width", width);
        properties.put("kaptcha.textproducer.font.size", fontSize);

        Config config = new Config(properties);
        DefaultKaptcha defaultKaptcha = new DefaultKaptcha();
        defaultKaptcha.setConfig(config);

        return defaultKaptcha;

    }

}
