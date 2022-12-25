package com.chiu.megalith.websocket.dto.impl;

import com.chiu.megalith.websocket.dto.Container;
import com.chiu.megalith.websocket.dto.MessageDto;
import com.chiu.megalith.websocket.vo.UserEntityVo;
import lombok.AllArgsConstructor;
import lombok.Data;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-10-14 6:15 AM
 */
@Data
@AllArgsConstructor
public class PushUserDto implements Serializable, MessageDto<PushUserDto.Bind> {
    private Container<Bind> data;

    @Override
    public Container<Bind> getData() {
        return data;
    }


    @Data
    @AllArgsConstructor
    public static class Bind implements Serializable {
        private Long blogId;
        private List<UserEntityVo> users;
    }
}

