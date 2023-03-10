package com.chiu.megalith.base.controller;

import com.chiu.megalith.base.lang.Result;
import lombok.RequiredArgsConstructor;
import org.springframework.amqp.rabbit.listener.MessageListenerContainer;
import org.springframework.amqp.rabbit.listener.RabbitListenerEndpointRegistry;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;


/**
 * @author mingchiuli
 * @create 2022-01-04 4:35 PM
 */
@Controller
@MessageMapping("/log")
@RequiredArgsConstructor
public class LogController {

    private final RabbitListenerEndpointRegistry registry;


    @MessageMapping("/start")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> start() {
        MessageListenerContainer logContainer = registry.getListenerContainer("log");

        if (!logContainer.isRunning()) {
            logContainer.start();
        }

        return Result.success();
    }

    @MessageMapping("/stop")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> stop() {
        MessageListenerContainer logContainer = registry.getListenerContainer("log");

        if (logContainer.isRunning()) {
            logContainer.stop();
        }

        return Result.success();
    }

}
