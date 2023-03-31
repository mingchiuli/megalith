package com.chiu.megalith.infra.lang;

import lombok.Data;

import java.io.Serializable;

/**
 * @author mingchiuli
 * @create 2021-10-27 3:27 PM
 */
@Data
public class Result<T> implements Serializable {

    private int status;

    private String msg;

    private T data;

    public static <T> Result<T> success(T data) {
        return load(200, "operate success",data); //200为正常，非200为非正常
    }

    public static <T> Result<T> success() {
        return load(200, "operate success",null); //200为正常，非200为非正常
    }

    private static <T> Result<T> load(int status, String msg, T data) {
        Result<T> r = new Result<>();
        r.setStatus(status);
        r.setData(data);
        r.setMsg(msg);
        return r;
    }
    public static <T> Result<T> fail(Integer status, String msg, T data) {
        return load(status, msg, data);
    }

    public static <T> Result<T> fail() {
        return load(400, null, null);
    }

    public static <T> Result<T> fail(String msg) {
        return load(400, msg, null);
    }

    public static Result<Object> fail(int status, String message) {
        return load(status, message, null);
    }
}
