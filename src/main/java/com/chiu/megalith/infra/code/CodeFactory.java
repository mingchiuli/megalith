package com.chiu.megalith.infra.code;

import com.chiu.megalith.infra.exception.CodeException;
import com.chiu.megalith.infra.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.concurrent.ThreadLocalRandom;

/**
 * @author mingchiuli
 * @create 2023-03-05 1:04 am
 */
@Component
@RequiredArgsConstructor
public class CodeFactory {
    private static final char[] code = {
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
            'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
            'u', 'v', 'w', 'x', 'y', 'z',
            '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'
    };

    private static final char[] sms = {
            '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'
    };

    private final StringRedisTemplate redisTemplate;

    public String create(String type) {
        if (Const.SMS_CODE.getInfo().equals(type)) {
            return createSMS();
        } else if (Const.EMAIL_CODE.getInfo().equals(type)) {
            return createEmailCode();
        }
        throw new CodeException("code type input error");
    }

    private String createEmailCode() {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < 5; i++) {
            int idx = ThreadLocalRandom.current().nextInt(code.length);
            builder.append(code[idx]);
        }
        return builder.toString();
    }

    private String createSMS() {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < 6; i++) {
            int idx = ThreadLocalRandom.current().nextInt(sms.length);
            builder.append(sms[idx]);
        }
        return builder.toString();
    }

    public void save(String code,
                     String prefix) {
        String lua = "redis.call('hmset', KEYS[1], ARGV[1], ARGV[2], ARGV[3], ARGV[4]);" +
                "redis.call('expire', KEYS[1], ARGV[5]);";

        RedisScript<Void> script = RedisScript.of(lua);
        redisTemplate.execute(script, Collections.singletonList(prefix),
                "code", code, "try_count", "0", "120");
    }
}
