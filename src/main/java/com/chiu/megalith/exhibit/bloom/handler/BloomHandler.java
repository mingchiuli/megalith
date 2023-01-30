package com.chiu.megalith.exhibit.bloom.handler;

public interface BloomHandler {

    boolean supports(Class<? extends BloomHandler> clazz);

    void handle(Object[] args);
}
