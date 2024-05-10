package org.chiu.megalith.security.convertor;

import org.chiu.megalith.user.vo.UserEntityVo;
import org.chiu.megalith.security.vo.UserInfoVo;

public class UserInfoVoConvertor {

    public static UserInfoVo convert(UserEntityVo userEntity) {
        return UserInfoVo.builder()
                .avatar(userEntity.getAvatar())
                .nickname(userEntity.getNickname())
                .build();
    }
}
