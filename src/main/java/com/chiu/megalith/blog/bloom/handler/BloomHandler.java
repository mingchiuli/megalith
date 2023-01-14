package com.chiu.megalith.blog.bloom.handler;

public interface BloomHandler {

    boolean supports(Class<? extends BloomHandler> clazz);

    void handle(Object[] args);
}
