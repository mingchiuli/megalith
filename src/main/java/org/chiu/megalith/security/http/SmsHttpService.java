package org.chiu.megalith.security.http;


import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.service.annotation.GetExchange;


public interface SmsHttpService {

    @GetExchange("/{paramStr}")
    void sendSms(@PathVariable String paramStr);
}
