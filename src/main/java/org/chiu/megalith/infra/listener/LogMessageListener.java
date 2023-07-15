package org.chiu.megalith.infra.listener;

import org.chiu.megalith.infra.config.LogRabbitConfig;
import lombok.RequiredArgsConstructor;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;


/**
 * logback日志内容塞到rabbitmq队列里
 * @author mingchiuli
 * @create 2022-01-03 8:56 PM
 */
@Component
@RequiredArgsConstructor
public class LogMessageListener {

    private final SimpMessagingTemplate simpMessagingTemplate;

    @RabbitListener(id = "log", queues = LogRabbitConfig.LOG_QUEUE, autoStartup = "false", ackMode = "AUTO")
    public void processMessage(String msg) {
        simpMessagingTemplate.convertAndSend("/logs/log", msg);
    }
}
