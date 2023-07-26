package org.chiu.megalith.infra.controller;

import org.chiu.megalith.infra.lang.Result;
import lombok.RequiredArgsConstructor;
import org.springframework.amqp.rabbit.listener.MessageListenerContainer;
import org.springframework.amqp.rabbit.listener.RabbitListenerEndpointRegistry;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;

import jakarta.annotation.PostConstruct;

/**
 * @author mingchiuli
 * @create 2022-01-04 4:35 PM
 */
@Controller
@MessageMapping("/log")
@RequiredArgsConstructor
public class LogController {

    private final RabbitListenerEndpointRegistry registry;

    private MessageListenerContainer logContainer;

    @PostConstruct
    private void setLogContainer() {
        logContainer = registry.getListenerContainer("log");
    }

    @MessageMapping("/start")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> start() {
        return Result.success(() -> {
            if (Boolean.FALSE.equals(logContainer.isRunning())) {
                logContainer.start();
            }
        });
    }

    @MessageMapping("/stop")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> stop() {
        return Result.success(() -> {
            if (logContainer.isRunning()) {
                logContainer.stop();
            }
        });
    }

}