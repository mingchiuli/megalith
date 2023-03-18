package com.chiu.megalith.security.user;

import com.chiu.megalith.base.lang.Const;
import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Component;


@Component
@RequiredArgsConstructor
public class UserDetailsServiceImpl implements UserDetailsService {

	private final UserRepository userRepository;

	@Override
	public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {

		UserEntity user = userRepository.findByUsernameOrEmailOrPhone(username, username, username).
				orElseThrow(() -> new UsernameNotFoundException("username not exist"));

		String grantType;

		if (username.contains("@")) {
			grantType = Const.GRANT_TYPE_EMAIL.getInfo();
		} else if (username.matches("\\d+")){
			grantType = Const.GRANT_TYPE_PHONE.getInfo();
		} else {
			grantType = Const.GRANT_TYPE_PASSWORD.getInfo();
		}

		//通过User去自动比较用户名和密码
		return new LoginUser(username,
				user.getPassword(),
				true,
				true,
				true,
				user.getStatus() == 0,
				AuthorityUtils.createAuthorityList("ROLE_" + user.getRole()),
				grantType);
	}
}
