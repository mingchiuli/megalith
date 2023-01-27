package com.chiu.megalith.ws.mq;

import com.chiu.megalith.common.config.LogRabbitConfig;
import com.rabbitmq.client.Channel;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

import java.io.IOException;

/**
 * logback日志内容塞到rabbitmq队列里
 * @author mingchiuli
 * @create 2022-01-03 8:56 PM
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class LogMessageRabbitListener {

    private final SimpMessagingTemplate simpMessagingTemplate;

    @RabbitListener(id = "log", queues = LogRabbitConfig.LOG_QUEUE, autoStartup = "false")
    public void processMessage(String msg, Channel channel, Message message) {
        long deliveryTag = message.getMessageProperties().getDeliveryTag();
        try {
            simpMessagingTemplate.convertAndSend("/logs/log", msg);
            channel.basicAck(deliveryTag, false);
        } catch (IOException e) {
            log.info("log exception", e);
        }
    }
}