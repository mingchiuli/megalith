package com.chiu.megalith.ws.service;

import com.chiu.megalith.blog.vo.BlogEntityVo;
import com.chiu.megalith.ws.vo.InitCoopVo;

/**
 * @author mingchiuli
 * @create 2022-12-26 1:05 am
 */
public interface CoopService {
    InitCoopVo initCoop(Long blogId, Integer orderNumber);

    void submit(Long blogId, BlogEntityVo blogEntityVo);

}
