package org.chiu.megalith.infra.exception;

/**
 * @author mingchiuli
 * @create 2022-07-07 11:06 AM
 */
public class NotFoundException extends RuntimeException {
    public NotFoundException(String message) {
        super(message);
    }
}
