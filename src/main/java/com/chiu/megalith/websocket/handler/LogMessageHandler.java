package com.chiu.megalith.websocket.handler;

import com.chiu.megalith.common.config.LogConfig;
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
public class LogMessageHandler {

    private final SimpMessagingTemplate simpMessagingTemplate;


    @RabbitListener(id = "log", queues = LogConfig.LOG_QUEUE, autoStartup = "false")
//    @RabbitListener(id = "log", queues = LogMQConfig.LOG_QUEUE)
    public void processMessage(String msg, Channel channel, Message message) {
        long deliveryTag = message.getMessageProperties().getDeliveryTag();
        //手动签收消息
        //false代表不是批量签收模式
        try {
            simpMessagingTemplate.convertAndSend("/logs/log", msg);
            channel.basicAck(deliveryTag, false);
        } catch (IOException e) {
            //出现异常说明网络中断
            log.info("log exception", e);
        }

        //退货，重新入队列
//        channel.basicNack(deliveryTag, false, true);
    }
}
