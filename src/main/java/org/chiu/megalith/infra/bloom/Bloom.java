package org.chiu.megalith.infra.bloom;


import org.chiu.megalith.infra.bloom.handler.BloomHandler;

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
