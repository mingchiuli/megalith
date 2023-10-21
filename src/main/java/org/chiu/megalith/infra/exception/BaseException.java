package org.chiu.megalith.infra.exception;

import lombok.Getter;
import org.chiu.megalith.infra.lang.ExceptionMessage;

@Getter
public class BaseException extends RuntimeException {

    private Integer code;

    public BaseException(String message) {
        super(message);
    }

    public BaseException(ExceptionMessage exceptionMessage) {
        super(exceptionMessage.getMsg());
        this.code = exceptionMessage.getCode();
    }
}
