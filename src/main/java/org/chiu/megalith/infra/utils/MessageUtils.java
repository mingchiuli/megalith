package org.chiu.megalith.infra.utils;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.infra.lang.Const;
import org.springframework.amqp.rabbit.connection.CorrelationData;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.concurrent.TimeUnit;

@Component
@RequiredArgsConstructor
public class MessageUtils {

    private final RabbitTemplate rabbitTemplate;

    private final StringRedisTemplate redisTemplate;

    public <T> void sendMessageOnce(String exchange, String routingKey, T messageBody, Object... onceKey) {
        String key = Arrays.stream(onceKey)
                .reduce((o, n) -> o + "_" + n)
                .orElse("")
                .toString();

        var correlationData = new CorrelationData();
        redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                key,
                30,
                TimeUnit.MINUTES);

        rabbitTemplate.convertAndSend(exchange, routingKey, messageBody, correlationData);
    }
}
