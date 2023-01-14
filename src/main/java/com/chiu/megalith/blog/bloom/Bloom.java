package com.chiu.megalith.blog.bloom;


import com.chiu.megalith.blog.bloom.handler.BloomHandler;

import java.lang.annotation.*;

/**
 * @author mingchiuli
 * @create 2022-06-07 11:00 AM
 */
@Target({ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface Bloom {
    Class<? extends BloomHandler> handler();
}
