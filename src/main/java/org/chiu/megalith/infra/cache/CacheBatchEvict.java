package org.chiu.megalith.infra.cache;

import org.chiu.megalith.infra.lang.Const;

import java.lang.annotation.*;

@Target({ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface CacheBatchEvict {

    Const[] prefix();
}
