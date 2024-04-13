package org.chiu.megalith.manage.service.impl;

import lombok.SneakyThrows;
import org.chiu.megalith.infra.code.CodeFactory;
import org.chiu.megalith.infra.http.OssHttpService;
import org.chiu.megalith.infra.cache.CacheBatchEvict;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.user.UserIndexMessage;
import org.chiu.megalith.infra.utils.OssSignUtils;
import org.chiu.megalith.manage.convertor.UserEntityVoConvertor;
import org.chiu.megalith.manage.entity.UserEntity;
import org.chiu.megalith.manage.event.UserOperateEvent;
import org.chiu.megalith.manage.repository.UserRepository;
import org.chiu.megalith.manage.req.UserEntityRegisterReq;
import org.chiu.megalith.manage.service.UserService;
import org.chiu.megalith.manage.req.UserEntityReq;
import org.chiu.megalith.infra.exception.CommitException;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.page.PageAdapter;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.vo.UserEntityVo;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.server.header.CacheControlServerHttpHeadersWriter;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.TimeUnit;

import static org.chiu.megalith.infra.lang.Const.*;
import static org.chiu.megalith.infra.lang.ExceptionMessage.*;
import static org.chiu.megalith.infra.lang.StatusEnum.NORMAL;

/**
 * @author mingchiuli
 * @create 2022-12-04 4:55 pm
 */
@Service
@RequiredArgsConstructor
public class UserServiceImpl implements UserService {

    private final PasswordEncoder passwordEncoder;

    private final UserRepository userRepository;

    private final ApplicationContext applicationContext;

    private final StringRedisTemplate redisTemplate;

    private final OssHttpService ossHttpService;

    private final OssSignUtils ossSignUtils;

    private final CodeFactory codeFactory;

    @Value("${blog.oss.base-url}")
    private String baseUrl;

    @Value("${blog.register.page-prefix}")
    private String pagePrefix;

    @Override
    public void updateLoginTime(String username, LocalDateTime time) {
        userRepository.updateLoginTime(username, time);
    }

    @Override
    @CacheBatchEvict(prefix = {Const.HOT_AUTHORITIES})
    public void saveOrUpdate(UserEntityReq userEntityReq) {
        Long id = userEntityReq.getId();
        UserEntity userEntity;
        var now = LocalDateTime.now();
        String roleLast = null;

        if (Objects.nonNull(id)) {
            userEntity = userRepository.findById(id)
                    .orElseThrow(() -> new MissException(USER_NOT_EXIST));

            roleLast = userEntity.getRole();
            String password = userEntityReq.getPassword();
            if (StringUtils.hasLength(password)) {
                userEntityReq.setPassword(passwordEncoder.encode(password));
            } else {
                userEntityReq.setPassword(userEntity.getPassword());
            }
        } else {
            userEntity = UserEntity.builder()
                    .created(now)
                    .lastLogin(now)
                    .build();
            userEntityReq.setPassword(
                    passwordEncoder.encode(Optional.ofNullable(userEntityReq.getPassword())
                                    .orElseThrow(() -> new CommitException(PASSWORD_REQUIRED))
                    )
            );
        }

        BeanUtils.copyProperties(userEntityReq, userEntity);
        userRepository.save(userEntity);

        var userIndexMessage = new UserIndexMessage(userEntity.getId(), userEntity.getRole(), roleLast);
        applicationContext.publishEvent(new UserOperateEvent(this, userIndexMessage));
    }

    @Override
    public UserEntityVo findById(Long userId) {
        UserEntity userEntity = userRepository.findById(userId)
                .orElseThrow(() -> new MissException(USER_NOT_EXIST));

        return UserEntityVoConvertor.convert(userEntity);
    }

    @Override
    public void changeUserStatusByUsername(String username, Integer status) {
        userRepository.setUserStatusByUsername(username, status);
    }

    @Override
    public PageAdapter<UserEntityVo> listPage(Integer currentPage, Integer size) {
        var pageRequest = PageRequest.of(currentPage - 1,
                size,
                Sort.by("created").ascending());
        Page<UserEntity> page = userRepository.findAll(pageRequest);

        return UserEntityVoConvertor.convert(page);
    }

    @Override
    public void deleteUsers(List<Long> ids) {
        userRepository.deleteAllById(ids);
    }

    @Override
    public List<Long> findIdsByStatus(Integer status) {
        return userRepository.findByStatus(status);
    }

    @Override
    public UserEntityVo findByEmail(String email) {
        UserEntity userEntity = userRepository.findByEmail(email)
                .orElseThrow(() -> new MissException(EMAIL_NOT_EXIST));

        return UserEntityVoConvertor.convert(userEntity);
    }

    @Override
    public UserEntityVo findByPhone(String loginSMS) {
        UserEntity userEntity = userRepository.findByPhone(loginSMS)
                .orElseThrow(() -> new MissException(SMS_NOT_EXIST));

        return UserEntityVoConvertor.convert(userEntity);
    }

    @Override
    public String getRegisterPage(String username) {
        String token = UUID.randomUUID().toString();
        redisTemplate.opsForValue().set(REGISTER_PREFIX.getInfo() + token, username, 1, TimeUnit.HOURS);
        return pagePrefix + token + "?username=" + username;
    }

    @Override
    public void saveRegisterPage(String token, UserEntityRegisterReq userEntityRegisterReq) {
        Boolean exist = redisTemplate.hasKey(REGISTER_PREFIX.getInfo() + token);
        if (Objects.isNull(exist) || !exist) {
            throw new BadCredentialsException(NO_AUTH.getMsg());
        }
        String password = userEntityRegisterReq.getPassword();
        String confirmPassword = userEntityRegisterReq.getConfirmPassword();
        if (!Objects.equals(confirmPassword, password)) {
            throw new MissException(PASSWORD_DIFF.getMsg());
        }

        String phone = userEntityRegisterReq.getPhone();
        if (!StringUtils.hasLength(phone)) {
            String fakePhone = codeFactory.create(PHONE_CODE.getInfo());
            userEntityRegisterReq.setPhone(fakePhone);
        }

        String username = userEntityRegisterReq.getUsername();
        String usernameCopy = redisTemplate.opsForValue().get(REGISTER_PREFIX.getInfo() + token);
        if (StringUtils.hasLength(usernameCopy) && !Objects.equals(usernameCopy, username)) {
            throw new BadCredentialsException(NO_AUTH.getMsg());
        }

        UserEntityReq userEntityReq = new UserEntityReq();

        Optional<UserEntity> userEntity = userRepository.findByUsernameAndStatus(username, NORMAL.getCode());
        userEntity.ifPresent(entity -> userEntityRegisterReq.setId(entity.getId()));

        BeanUtils.copyProperties(userEntityRegisterReq, userEntityReq);
        userEntityReq.setRole(USER.getInfo());
        userEntityReq.setStatus(NORMAL.getCode());
        saveOrUpdate(userEntityReq);
        redisTemplate.delete(REGISTER_PREFIX.getInfo() + token);
    }

    @SneakyThrows
    @Override
    public String imageUpload(String token, MultipartFile image) {
        Boolean exist = redisTemplate.hasKey(REGISTER_PREFIX.getInfo() + token);
        if (Objects.isNull(exist) || !exist) {
            throw new BadCredentialsException(NO_AUTH.getMsg());
        }

        Assert.notNull(image, UPLOAD_MISS.getMsg());
        String uuid = UUID.randomUUID().toString();
        String originalFilename = image.getOriginalFilename();
        originalFilename = Optional.ofNullable(originalFilename)
                .orElseGet(() -> UUID.randomUUID().toString())
                .replace(" ", "");
        String objectName = "avatar/" + uuid + "-" + originalFilename;
        byte[] imageBytes = image.getBytes();

        Map<String, String> headers = new HashMap<>();
        String gmtDate = ossSignUtils.getGMTDate();
        headers.put(HttpHeaders.DATE, gmtDate);
        headers.put(HttpHeaders.AUTHORIZATION, ossSignUtils.getAuthorization(objectName, HttpMethod.PUT.name(), "image/jpg"));
        headers.put(HttpHeaders.CACHE_CONTROL, CacheControlServerHttpHeadersWriter.PRAGMA_VALUE);
        headers.put(HttpHeaders.CONTENT_TYPE, "image/jpg");
        ossHttpService.putOssObject(objectName, imageBytes, headers);
        return baseUrl + "/" + objectName;
    }

    @Override
    public void imageDelete(String token, String url) {
        Boolean exist = redisTemplate.hasKey(REGISTER_PREFIX.getInfo() + token);
        if (Objects.isNull(exist) || !exist) {
            throw new BadCredentialsException(NO_AUTH.getMsg());
        }
        String objectName = url.replace(baseUrl + "/", "");
        Map<String, String> headers = new HashMap<>();
        String gmtDate = ossSignUtils.getGMTDate();
        headers.put(HttpHeaders.DATE, gmtDate);
        headers.put(HttpHeaders.AUTHORIZATION, ossSignUtils.getAuthorization(objectName, HttpMethod.DELETE.name(), ""));
        ossHttpService.deleteOssObject(objectName, headers);
    }

    @Override
    public Boolean checkRegisterPage(String token) {
        return redisTemplate.hasKey(REGISTER_PREFIX.getInfo() + token);
    }
}
