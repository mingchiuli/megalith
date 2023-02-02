package com.chiu.megalith.common.listener;

import com.chiu.megalith.common.config.LogRabbitConfig;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;


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

    @RabbitListener(
            id = "log",
            queues = LogRabbitConfig.LOG_QUEUE,
            autoStartup = "false",
            ackMode = "AUTO")
    public void processMessage(String msg) {
        simpMessagingTemplate.convertAndSend("/logs/log", msg);
    }
}
