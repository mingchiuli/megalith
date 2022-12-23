package com.chiu.megalith.common.config;

import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.context.annotation.Configuration;

/**
 * @author mingchiuli
 * @create 2022-12-23 12:32 pm
 */
@Configuration
@RequiredArgsConstructor
@Slf4j
public class RabbitMQCallbackConfig {

    private final RabbitTemplate rabbitTemplate;

    @PostConstruct
    public void initRabbitTemplate() {
        //设置抵达broker服务器的回掉
        //当前消息的唯一关联数据、服务器对消息是否成功收到、失败的原因
        rabbitTemplate.setConfirmCallback((correlationData, ack, cause) -> {
            log.info("message not come to mq {}, {}, {}", correlationData, ack, cause);
        });

        //设置抵达消息队列的确认回调
        //只要消息没有投递给指定的队列，就触发这个失败回调
        rabbitTemplate.setReturnsCallback(returned -> {
            log.info("message not come to queue {}", returned);
        });

    }
}
