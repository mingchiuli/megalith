package com.chiu.megalith.blog.bloom.handler;

public interface BloomHandler {

    boolean supports(Class<?> handler);

    void handle(Object[] args);
}
